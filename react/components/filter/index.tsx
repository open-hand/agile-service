import { IFieldType, ISystemFieldCode } from '@/common/types';
import Filter from './Filter';

export interface IFilter {
  [key: string]: any
}
export interface ISystemField {
  code: ISystemFieldCode,
  title: string,
  fieldType: IFieldType,
  required?: boolean,
  system: true
  nameKey?: string
}
interface IFieldOption {
  id: string
  value: string
  enabled: boolean
}
export interface ICustomField {
  id: string
  code: string
  title: string
  fieldType: IFieldType
  required: boolean
  fieldOptions?: IFieldOption[]
  value?: string
  valueStr?: string | any
  system: boolean
}
export type IFilterField = ISystemField | ICustomField

export default Filter;
