import { set } from 'lodash';
import { toJS } from 'mobx';
import { ILocalField } from '@/components/issue-search/store';
import { transformFilter } from '@/routes/Issue/stores/utils';

export function getWorkbenchSystemFields() {
  return [{
    code: 'contents',
    name: '概要',
    defaultShow: true,
    noDisplay: true,
  }, {
    code: 'projectIds',
    name: '所属项目',
    defaultShow: true,
    fieldType: 'multiple',
  }, {
    code: 'issueTypeId',
    name: '工作项类型',
    defaultShow: true,
    fieldType: 'multiple',
  }, {
    code: 'statusId',
    name: '状态',
    defaultShow: true,
    fieldType: 'multiple',
  }, {
    code: 'assigneeId',
    name: '经办人',
    defaultShow: true,
    fieldType: 'member',
  }] as ILocalField[];
}
export function transformWorkbenchFilter(data: any = {}) {
  const search = transformFilter(data);
  const { value: projectIds } = toJS(data.get('projectIds')) || { value: [] };
  set(search, 'searchArgs.projectIds', projectIds);
  return search;
}
