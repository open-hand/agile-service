import { userApi } from '@/api';

export enum SelectType {
  user = 'user'
}
export interface TypeConfig {
  name: SelectType
  textField?: string
  valueField?: string
  render?: (item: any) => JSX.Element
  request: (search: string) => Promise<any>
  paging?: boolean
  props?: object
}
const types = new Map<SelectType, TypeConfig>([
  [SelectType.user, {
    name: SelectType.user,
    textField: 'realName',
    valueField: 'id',
    request: search => userApi.getAllInProject(search),
  }],
]);

export default types;
