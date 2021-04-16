export interface IFastSearchCondition {
    name: string
    fieldCode: string
    relation: string
    valueBindValue?: string | string[]
    value: any
    fieldType: string
    valueText?: string | string[]
    isCustomField: boolean
    bothRelation?: string
}
interface IFastSearchConditionWithEditStatus extends Omit<IFastSearchCondition, 'valueText' | 'valueBindValue' | 'name'> {
    _editData: true
}
export interface IFastSearchEditData {
    filterId: string
    name: string
    description: string
    objectVersionNumber: number
    projectId: string
    childIncluded: boolean
    searchConditionList: IFastSearchConditionWithEditStatus[]
}
