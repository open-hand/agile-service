import { IFieldType } from '@/common/types';

export type IFieldTypeWithSystemType = IFieldType | 'long' | 'decimal' | 'date'

export interface IFastSearchCondition {
    name: string
    fieldCode: string
    relation: string
    valueBindValue?: string | string[]
    value: any
    fieldType: IFieldTypeWithSystemType
    valueText?: string | string[]
    isCustomField: boolean
    bothRelation?: string
}
export interface IFastSearchEditConditionWithEditStatus extends Omit<IFastSearchCondition, 'valueText' | 'valueBindValue' | 'name' | 'fieldType'> {
    _editData: true
    _editDataCode?: IFastSearchEditConditionWithEditStatus['fieldCode']
}
export interface IFastSearchEditData {
    filterId: string
    name: string
    description: string
    objectVersionNumber: number
    projectId: string
    childIncluded: boolean
    searchConditionList: IFastSearchEditConditionWithEditStatus[]
}
