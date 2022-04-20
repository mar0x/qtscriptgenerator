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

#include "docgenerator.h"
#include "fileout.h"

DocGenerator::DocGenerator()
{
}

QString DocGenerator::fileNameForClass(const AbstractMetaClass *meta_class) const
{
    return QString::fromLatin1("%0.html").arg(meta_class->name().toLower());
}

QString DocGenerator::subDirectoryForClass(const AbstractMetaClass *) const
{
    return QString::fromLatin1("doc");
}

static void writeDocumentHeader(QTextStream &s, const QString &title)
{
    s << "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>" << Qt::endl
      << "<!DOCTYPE html" << Qt::endl
      << "    PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"DTD/xhtml1-strict.dtd\">" << Qt::endl
      << "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">" << Qt::endl
      << "<head>" << Qt::endl
      << "  <title>" << title << "</title>" << Qt::endl
      << "  <link href=\"classic.css\" rel=\"stylesheet\" type=\"text/css\" />" << Qt::endl
      << "</head>" << Qt::endl
      << "<body>" << Qt::endl;
}

static void writeDocumentFooter(QTextStream &s)
{
    s << "</body>" << Qt::endl
      << "</html>" << Qt::endl;
}

static bool classLessThan(const AbstractMetaClass *c1, const AbstractMetaClass *c2)
{
    return c1->name() < c2->name();
}

bool DocGenerator::shouldGenerate(const AbstractMetaClass *meta_class) const
{
    uint cg = meta_class->typeEntry()->codeGeneration();
    return (cg & TypeEntry::GenerateCode) != 0;
}

void DocGenerator::generate()
{
    Generator::generate();

    QHash<QString, QList<const AbstractMetaClass*> > packHash;
    for (int i = 0; i < m_classes.size(); ++i) {
        const AbstractMetaClass *cls = m_classes.at(i);
        packHash[cls->package()].append(cls);
    }

    // package pages
    QHash<QString, QList<const AbstractMetaClass*> >::const_iterator it;
    for (it = packHash.constBegin(); it != packHash.constEnd(); ++it) {
        QString package = it.key();
        QList<const AbstractMetaClass*> classesInPackage = it.value();
        std::sort(classesInPackage.begin(), classesInPackage.end(), classLessThan);

        FileOut file(m_out_dir + "/doc/" + package.split(".").join("_") + ".html");

        writeDocumentHeader(file.stream, package + " Package");

        file.stream << "<h1 align=\"center\">" << package << " Package</h1>" << Qt::endl;

        file.stream << "<h2>Classes</h2>" << Qt::endl
                    << "<p><table width=\"100%\" class=\"annotated\" cellpadding=\"2\" cellspacing=\"1\" border=\"0\">" << Qt::endl;

        for (int i = 0; i < classesInPackage.size(); ++i) {
            const AbstractMetaClass *cls = classesInPackage.at(i);
            if (cls->name() == "Global")
                continue; /// ### fixme
            file.stream << "<tr valign=\"top\" class=\"";
            if (i & 1)
                file.stream << "odd";
            else
                file.stream << "even";
            file.stream << "\"><th><a href=\"" << fileNameForClass(cls) << "\">" << cls->name()
                        << "</a></th></tr>" << Qt::endl;
        }

        file.stream << "</table></p>" << Qt::endl;

        writeDocumentFooter(file.stream);
    }

    // all classes page
    {
        FileOut file(m_out_dir + "/doc/classes.html");

        writeDocumentHeader(file.stream, "Classes");

        file.stream << "<h1 align=\"center\">Classes</h1>" << Qt::endl
                    << "<p><table width=\"100%\" class=\"annotated\" cellpadding=\"2\" cellspacing=\"1\" border=\"0\">" << Qt::endl;

        AbstractMetaClassList sortedClasses = m_classes;
        std::sort(sortedClasses.begin(), sortedClasses.end(), classLessThan);

        for (int i = 0; i < sortedClasses.size(); ++i) {
            const AbstractMetaClass *cls = sortedClasses.at(i);
            if (cls->name() == "Global")
                continue; /// ### fixme
            file.stream << "<tr valign=\"top\" class=\"";
            if (i & 1)
                file.stream << "odd";
            else
                file.stream << "even";
            file.stream << "\"><th><a href=\"" << fileNameForClass(cls) << "\">" << cls->name()
                        << "</a></th></tr>" << Qt::endl;
        }

        file.stream << "</table></p>" << Qt::endl;

        writeDocumentFooter(file.stream);
    }

    // index.html
    {
        FileOut file(m_out_dir + "/doc/index.html");

        writeDocumentHeader(file.stream, "Qt Bindings Reference Documentation");

        file.stream << "<h1 align=\"center\">Qt Script Qt Bindings Reference Documentation</h1>" << Qt::endl;

        file.stream << "<h3>Packages</h3>" << Qt::endl;
        file.stream << "<ul>" << Qt::endl;
        QStringList sortedPackages = packHash.keys();
        std::sort(sortedPackages.begin(), sortedPackages.end());
        for (int i = 0; i < sortedPackages.size(); ++i) {
            QString pkg = sortedPackages.at(i);
            file.stream << "<li><b><a href=\"" << pkg.split(".").join("_") << ".html\">"
                        << pkg << "</a></b></li>" << Qt::endl;
        }
        file.stream << "</ul>" << Qt::endl;

        file.stream << "<h3><a href=\"classes.html\">All Classes</a></h3>" << Qt::endl;

        file.stream << "<h3><a href=\"../examples\">Examples</a></h3>" << Qt::endl;

        file.stream << "<h3>Getting Started</h3>" << Qt::endl
                    << "<p>Using the Qt API in Qt Script is very similar to C++." << Qt::endl
                    << "<pre>var f = new QFile(\"foo.txt\");</pre>" << Qt::endl
                    << "C++ enum values are mapped to properties of the script constructor function; e.g. " << Qt::endl
                    << "QIODevice::ReadOnly becomes QIODevice.ReadOnly.</p>" << Qt::endl
                    << "<pre>f.open(new QIODevice.OpenMode(QIODevice.ReadOnly));</pre>" << Qt::endl
                    << "<p>Each C++ flag type is mapped to a property of the script constructor function; e.g. " << Qt::endl
                    << "QIODevice::OpenMode becomes QIODevice.OpenMode. Such a property is a constructor function " << Qt::endl
                    << "that takes one or more enum values and constructs a flags instance by OR'ing the arguments " << Qt::endl
                    << "together.</p>" << Qt::endl
                    << "<pre>var ts = new QTextStream(f);" << Qt::endl
                    << "ts.writeString(\"Boo\");</pre>" << Qt::endl
                    << "<p>C++ streaming operators are normally mapped to readT() and writeT() functions.</p>" << Qt::endl
                    << "<pre>f.close();</pre>" << Qt::endl
                    << "<p>In Qt Script, all objects are allocated on the heap; objects that are no longer " << Qt::endl
                    << "referenced are garbage collected sometime in the future; therefore, make sure to " << Qt::endl
                    << "explicitly free up resources if you can. (Without the call to close(), the underlying " << Qt::endl
                    << "file would remain open until the file object is garbage collected.)</p>" << Qt::endl
            ;

        file.stream << "<h3><a href=\"http://doc.trolltech.com/latest\">Qt Reference Documentation</a></h3>" << Qt::endl;

        writeDocumentFooter(file.stream);
    }
}

static bool shouldIgnoreEnum(const AbstractMetaEnum *enom)
{
    return !enom->wasPublic() || (enom->name() == "enum_1");
}

// in classgenerator.cpp
void findPrototypeAndStaticFunctions(
    const AbstractMetaClass *meta_class,
    QMap<QString, AbstractMetaFunctionList> &nameToPrototypeFunctions,
    QMap<QString, AbstractMetaFunctionList> &nameToStaticFunctions);
QList<int> uniqueEnumValueIndexes(const AbstractMetaEnumValueList &values);

static void writeFunction(QTextStream &s, const AbstractMetaFunction *fun)
{
    s << "<li><div class=\"fn\"/><b>" << fun->targetLangSignature() << "</b></li>" << Qt::endl;
}

void DocGenerator::write(QTextStream &s, const AbstractMetaClass *meta_class)
{
    QString title = meta_class->name();
    title.append(" ");
    if (meta_class->isNamespace())
        title.append("Namespace");
    else
        title.append("Class");
    title.append(" Reference");
    writeDocumentHeader(s, title);

    s << "<h1 align=\"center\">" << title << "</h1>" << Qt::endl;

    s << "<h3 align=\"center\">[<a href=\"";
    s << meta_class->package().split(".").join("_") << ".html";
    s << "\">";
    s << meta_class->package();
    s << "</a> package]</h3>" << Qt::endl;

    if (meta_class->baseClass()) {
        s << "<p>Inherits <a href=\"" << fileNameForClass(meta_class->baseClass()) << "\">"
          << meta_class->baseClass()->name() << "</a>.</p>" << Qt::endl;
    } else if (!meta_class->interfaces().isEmpty()) {
        AbstractMetaClass *iface = meta_class->interfaces().first();
        AbstractMetaClass *impl = iface->primaryInterfaceImplementor();
        if (impl != meta_class) {
            s << "<p>Inherits <a href=\"" << fileNameForClass(impl) << "\">"
              << impl->name() << "</a>.</p>" << Qt::endl;
        }
    }

    AbstractMetaFunctionList ctors;
    ctors = meta_class->queryFunctions(AbstractMetaClass::Constructors
                                       | AbstractMetaClass::WasPublic
                                       | AbstractMetaClass::NotRemovedFromTargetLang);
    QMap<QString, AbstractMetaFunctionList> nameToPrototypeFunctions;
    QMap<QString, AbstractMetaFunctionList> nameToStaticFunctions;
    findPrototypeAndStaticFunctions(meta_class, nameToPrototypeFunctions, nameToStaticFunctions);

    s << "<h3>Constructor</h3>" << Qt::endl;
    if (!ctors.isEmpty()) {
        s << "<ul>" << Qt::endl;
        for (int i = 0; i < ctors.size(); ++i) {
            writeFunction(s, ctors.at(i));
        }
        s << "</ul>" << Qt::endl;
    } else {
        s << "<p>This class has no public constructors. Calling the constructor function will cause a TypeError.</p>";
    }

    s << "<h3>Constructor Properties</h3>" << Qt::endl;
    s << "<ul>" << Qt::endl;
    s << "<li><b>prototype</b>: The " << meta_class->name() << " prototype object</li>" << Qt::endl;
    if (!nameToStaticFunctions.isEmpty()) {
        QMap<QString, AbstractMetaFunctionList>::const_iterator it;
        for (it = nameToStaticFunctions.constBegin(); it != nameToStaticFunctions.constEnd(); ++it) {
            writeFunction(s, it.value().first());
        }
    }
    {
        AbstractMetaEnumList enums = meta_class->enums();
        for (int i = 0; i < enums.size(); ++i) {
            const AbstractMetaEnum *enom = enums.at(i);
            if (shouldIgnoreEnum(enom))
                continue;
            AbstractMetaEnumValueList values = enom->values();
            QList<int> indexes = uniqueEnumValueIndexes(values);
            for (int j = 0; j < indexes.size(); ++j) {
                AbstractMetaEnumValue *val = values.at(indexes.at(j));
                s << "<li><b>" << val->name();
                if (!val->stringValue().isEmpty())
                    s << " = " << val->stringValue();
                s << "</b></li>" << Qt::endl;
            }
            s << "<li><b>" << enom->name() << "( value )</b></li>" << Qt::endl;
            FlagsTypeEntry *flags = enom->typeEntry()->flags();
            if (flags)
                s << "<li><b>" << flags->flagsName() << "( value1, value2, ... )</b></li>" << Qt::endl;
        }
    }
    s << "</ul>" << Qt::endl;

    if (!nameToPrototypeFunctions.isEmpty()) {
        s << "<h3>Prototype Object Properties</h3>" << Qt::endl;
        if (meta_class->baseClass()) {
            s << "<p>The " << meta_class->name() << " prototype object inherits properties from the "
              << "<a href=\"" << fileNameForClass(meta_class->baseClass()) << "\">"
              << meta_class->baseClass()->name() << "</a> prototype object and "
              << "also has the following properties.</p>" << Qt::endl;
        }
        s << "<ul>" << Qt::endl;
        QMap<QString, AbstractMetaFunctionList>::const_iterator it;
        for (it = nameToPrototypeFunctions.constBegin(); it != nameToPrototypeFunctions.constEnd(); ++it) {
            writeFunction(s, it.value().first());
        }
        s << "</ul>" << Qt::endl;
    }

    if (!meta_class->isNamespace()) {
        s << "<h3>Instance Properties</h3>" << Qt::endl;
        {
            QList<QPropertySpec *> props = meta_class->propertySpecs();
            if (!props.isEmpty()) {
                s << "<p>" << meta_class->name() << " objects inherit properties from the "
                  << meta_class->name() << " prototype object and also have the following properties.</p>" << Qt::endl;
                s << "<ul>" << Qt::endl;
                for (int i = 0; i < props.size(); ++i) {
                    s << "<li><div class=\"fn\"/><b>" << props.at(i)->name() << "</b></li>" << Qt::endl;
                }
                s << "</ul>" << Qt::endl;
            } else {
                s << "<p>" << meta_class->name() << " objects have no special properties beyond those "
                  << "inherited from the " << meta_class->name() << " prototype object.</p>" << Qt::endl;
            }
        }
    }

    writeDocumentFooter(s);
}
