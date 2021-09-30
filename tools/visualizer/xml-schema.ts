import { equalsURI } from './triple'
import { Namespace, URI } from './uri'

const XML_SCHEMA_URL_DOMAIN = 'http://www.w3.org/2001'
const XML_SCHEMA_NAMESPACE = 'XMLSchema'


const XMLSchemaNamespace: Namespace = {
    domain: XML_SCHEMA_URL_DOMAIN,
    namespace: XML_SCHEMA_NAMESPACE,
}


function xmlSchema(name: string): URI {
    return {
        ns: XMLSchemaNamespace,
        name,
    }
}


export const XMLSchemaBoolean = xmlSchema('boolean')
export const XMLSchemaDate = xmlSchema('date')
export const XMLSchemaDateTime = xmlSchema('dateTime')
export const XMLSchemaDecimal = xmlSchema('decimal')
export const XMLSchemaFloat = xmlSchema('float')
export const XMLSchemaInt = xmlSchema('int')
export const XMLSchemaString = xmlSchema('string')


export function isXMLSchemaType(u: URI): boolean {
    return [
        XMLSchemaBoolean,
        XMLSchemaDate,
        XMLSchemaDateTime,
        XMLSchemaFloat,
        XMLSchemaInt,
        XMLSchemaString,
    ].some(xml => equalsURI(u, xml))
}
