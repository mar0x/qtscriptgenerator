/****************************************************************************
**
** Copyright (C) 2008-2009 Nokia Corporation and/or its subsidiary(-ies).
** All rights reserved.
** Contact: Nokia Corporation (qt-info@nokia.com)
**
** This file is part of the Qt Script Generator project on Qt Labs.
**
** $QT_BEGIN_LICENSE:LGPL$
** No Commercial Usage
** This file contains pre-release code and may not be distributed.
** You may use this file in accordance with the terms and conditions
** contained in the Technology Preview License Agreement accompanying
** this package.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU Lesser General Public License version 2.1 requirements
** will be met: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Nokia gives you certain additional
** rights.  These rights are described in the Nokia Qt LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
** If you have questions regarding the use of this file, please contact
** Nokia at qt-info@nokia.com.
**
**
**
**
**
**
**
**
** $QT_END_LICENSE$
**
****************************************************************************/

#include "setupgenerator.h"
#include "reporthandler.h"
#include "fileout.h"

//#define Q_SCRIPT_LAZY_GENERATOR

void SetupGenerator::addClass(const AbstractMetaClass *cls) 
{
    packHash[cls->package()].append(cls);
}

void writeQtScriptQtBindingsLicense(QTextStream &stream);

void maybeDeclareMetaType(QTextStream &stream, const QString &typeName,
                          QSet<QString> &registeredTypeNames);
bool hasDefaultConstructor(const AbstractMetaClass *meta_class);

bool clsNameLess(const AbstractMetaClass* a, const AbstractMetaClass* b)
{
    return a->qualifiedCppName() < b->qualifiedCppName();
}

void SetupGenerator::generate()
{
    QHashIterator<QString, QList<const AbstractMetaClass*> > pack(packHash);
    while (pack.hasNext()) {
        pack.next();
        QList<const AbstractMetaClass*> list = pack.value();
        if (list.isEmpty())
            continue;

        std::sort(list.begin(), list.end(), clsNameLess);

        QString packName = pack.key();
        packName.replace(".", "_");

        {
            FileOut initFile(m_out_dir + "/generated_cpp/" + packName + "/" + packName + "_init.cpp");
            QTextStream &s = initFile.stream;

            if (FileOut::license)
                writeQtScriptQtBindingsLicense(s);

            s << "#include <QtScript/QScriptValue>" << Qt::endl
              << "#include <QtScript/QScriptEngine>" << Qt::endl;
#ifdef Q_SCRIPT_LAZY_GENERATOR
            s << "#include <QtScript/QScriptClass>" << Qt::endl
              << "#include <QtScript/QScriptString>" << Qt::endl;
#endif
            s << Qt::endl;

            // declare individual class creation functions
            foreach (const AbstractMetaClass *cls, list) {
                s << "QScriptValue qtscript_create_" << cls->name() << "_class(QScriptEngine *engine);" << Qt::endl;
            }
            s << Qt::endl;

            // write table of class names
            {
                s << "static const char * const qtscript_" << packName << "_class_names[] = {" << Qt::endl;
                bool needComma = false;
                foreach (const AbstractMetaClass *cls, list) {
                    s << "    ";
                    if (needComma)
                        s << ", ";
                    s << "\"" << cls->name() << "\"" << Qt::endl;
                    needComma = true;
                }
                s << "};" << Qt::endl << Qt::endl;
            }

            // write table of function pointers
            {
                s << "typedef QScriptValue (*QtBindingCreator)(QScriptEngine *engine);" << Qt::endl;
                s << "static const QtBindingCreator qtscript_" << packName << "_class_functions[] = {" << Qt::endl;
                bool needComma = false;
                foreach (const AbstractMetaClass *cls, list) {
                    s << "    ";
                    if (needComma)
                        s << ", ";
                    s << "qtscript_create_" << cls->name() << "_class" << Qt::endl;
                    needComma = true;
                }
                s << "};" << Qt::endl << Qt::endl;
            }

#ifdef Q_SCRIPT_LAZY_GENERATOR
            {
                // declare meta-types
                QSet<QString> registeredTypeNames = m_qmetatype_declared_typenames;
                foreach (const AbstractMetaClass *cls, list) {
                    if (cls->isNamespace())
                        continue;
                    QString name = cls->qualifiedCppName();
                    if (cls->typeEntry()->isValue() && ::hasDefaultConstructor(cls))
                        maybeDeclareMetaType(s, name, registeredTypeNames);
                    maybeDeclareMetaType(s, name + "*", registeredTypeNames);
                }
                s << Qt::endl;
                // write table of metatype-ids
                s << "static const int qtscript_" << packName << "_metatype_ids[] = {" << Qt::endl;
                for (int i = 0; i < list.size(); ++i) {
                    const AbstractMetaClass *cls = list.at(i);
                    s << "    ";
                    if (i > 0)
                        s << ", ";
                    if (cls->isNamespace()) {
                        s << "-1, -1";
                    } else {
                        QString name = cls->qualifiedCppName();
                        if (cls->typeEntry()->isValue() && ::hasDefaultConstructor(cls))
                            s << "qMetaTypeId<" << name << ">()";
                        else
                            s << "-1";
                        s << ", qMetaTypeId<" << name << "*>()";
                    }
                    s << Qt::endl;
                }
                s << "};" << Qt::endl << Qt::endl;
            }

            // write the fake prototype class
            {
                s << "class qtscript_" << packName << "_FakePrototype : public QScriptClass" << Qt::endl
                  << "{" << Qt::endl
                  << "public:" << Qt::endl
                  << "    qtscript_" << packName << "_FakePrototype(QScriptEngine *engine)" << Qt::endl
                  << "        : QScriptClass(engine) {}" << Qt::endl << Qt::endl

                  << "    QueryFlags queryProperty(const QScriptValue &fake," << Qt::endl
                  << "        const QScriptString &name, QueryFlags flags, uint *)" << Qt::endl
                  << "    {" << Qt::endl
                  << "        if (fake.prototype().isValid())" << Qt::endl
                  << "            return 0;" << Qt::endl
                  << "        int classIndex = fake.data().toInt32();" << Qt::endl
                  << "        const char *className = qtscript_" << packName << "_class_names[classIndex];" << Qt::endl
//              << "        qDebug() << \"faking\" << className;" << Qt::endl
                  << "        QScriptValue extensionObject = engine()->globalObject();" << Qt::endl
                  << "        QScriptValue ctor = extensionObject.property(className);" << Qt::endl
                  << "        QScriptValue genuine = ctor.property(\"prototype\");" << Qt::endl
                  << "        Q_ASSERT(genuine.isObject());" << Qt::endl
                  << "        const_cast<QScriptValue&>(fake).setPrototype(genuine);" << Qt::endl
                  << "        if (!genuine.property(name).isValid())" << Qt::endl
                  << "            flags &= ~HandlesReadAccess;" << Qt::endl
                  << "        return flags & ~HandlesWriteAccess;" << Qt::endl
                  << "    }" << Qt::endl << Qt::endl

                  << "    QScriptValue property(const QScriptValue &fake, "
                  << "const QScriptString &name, uint)" << Qt::endl
                  << "    {" << Qt::endl
                  << "        return fake.prototype().property(name, QScriptValue::ResolveLocal);" << Qt::endl
                  << "    }" << Qt::endl
                  << "};" << Qt::endl << Qt::endl;
            }

            // write the lazy class loader
            {
                s << "static QScriptValue qtscript_" << packName << "_getSetClass("
                  << "QScriptContext *context, QScriptEngine *engine)" << Qt::endl
                  << "{" << Qt::endl
                  << "    QScriptValue target = context->thisObject();" << Qt::endl
                  << "    int classIndex = context->callee().data().toInt32();" << Qt::endl
                  << "    const char *className = qtscript_" << packName << "_class_names[classIndex];" << Qt::endl
                  << "    qDebug() << \"loading\" << className;" << Qt::endl
                  << "    target.setProperty(className, QScriptValue(), "
                  << "QScriptValue::PropertyGetter|QScriptValue::PropertySetter);" << Qt::endl
                  << "    if (context->argumentCount() == 1) {" << Qt::endl
                  << "        target.setProperty(className, context->argument(0));" << Qt::endl
                  << "    } else {" << Qt::endl
                  << "        target.setProperty(className, qtscript_"
                  << packName << "_class_functions[classIndex](engine)," << Qt::endl
                  << "            QScriptValue::SkipInEnumeration);" << Qt::endl
                  << "    }" << Qt::endl
                  << "    return target.property(className);" << Qt::endl
                  << "}" << Qt::endl << Qt::endl;
            }
#endif

            // bindings init function
            s << "void qtscript_initialize_" << packName << "_bindings(QScriptValue &extensionObject)" << Qt::endl
              << "{" << Qt::endl
              << "    QScriptEngine *engine = extensionObject.engine();" << Qt::endl;
#ifdef Q_SCRIPT_LAZY_GENERATOR
            s << "    qtscript_" << packName << "_FakePrototype *fakeProtoClass;" << Qt::endl
              << "    fakeProtoClass = new qtscript_" << packName << "_FakePrototype(engine);" << Qt::endl;
#endif
            s << "    for (int i = 0; i < " << list.size() << "; ++i) {" << Qt::endl
#ifndef Q_SCRIPT_LAZY_GENERATOR
              << "        extensionObject.setProperty(qtscript_" << packName << "_class_names[i]," << Qt::endl
              << "            qtscript_" << packName << "_class_functions[i](engine)," << Qt::endl
              << "            QScriptValue::SkipInEnumeration);" << Qt::endl
#else
              << "        QScriptValue classIndex(engine, i);" << Qt::endl
              << "        QScriptValue fakeCtor = engine->newFunction(qtscript_" << packName << "_getSetClass);" << Qt::endl
              << "        fakeCtor.setData(classIndex);" << Qt::endl
              << "        extensionObject.setProperty(qtscript_" << packName << "_class_names[i]," << Qt::endl
              << "            fakeCtor, QScriptValue::PropertyGetter|QScriptValue::PropertySetter"
              << "|QScriptValue::SkipInEnumeration);" << Qt::endl
              << "        QScriptValue fakeProto = engine->newObject(fakeProtoClass, classIndex);" << Qt::endl
              << "        fakeProto.setPrototype(QScriptValue());" << Qt::endl
              << "        if (qtscript_" << packName << "_metatype_ids[i*2] != -1)" << Qt::endl
              << "            engine->setDefaultPrototype(qtscript_"
              << packName << "_metatype_ids[i*2], fakeProto);" << Qt::endl
              << "        if (qtscript_" << packName << "_metatype_ids[i*2+1] != -1)" << Qt::endl
              << "            engine->setDefaultPrototype(qtscript_"
              << packName << "_metatype_ids[i*2+1], fakeProto);" << Qt::endl
#endif
              << "    }" << Qt::endl
              << "}" << Qt::endl;

            if (initFile.done())
                ++m_num_generated_written;
            ++m_num_generated;
        }
    }
}
