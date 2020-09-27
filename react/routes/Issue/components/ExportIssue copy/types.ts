interface IExportIssueField {
    id?: string, // 自定义字段具有id
    code: string,
    name: string,
    defaultShow?: boolean,
    fieldType?: string,
    noDisplay?: boolean,
    immutableCheck?:boolean,
    fieldOptions?: Array<{ id: string, value: string, enabled: boolean }>,
}
interface IExportIssueChosenField extends IExportIssueField {
    value: any,
}

export { IExportIssueField, IExportIssueChosenField };
