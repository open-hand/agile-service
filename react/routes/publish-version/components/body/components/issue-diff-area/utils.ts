import { publishVersionApi } from '@/api';
import { Issue } from '@/common/types';
import {
  set, omit, get, isEmpty,
} from 'lodash';
import { toJS } from 'mobx';

async function sequenceRequest(requests: Array<() => Promise<any[]>>, tagData: any[], i: number): Promise<any[]> {
  let data = [];
  if (requests[i]) { /** 0[5]  --> 1[4] */
    data = await requests[i]().then(async (res) => {
      console.log('nextData', res);
      const currentData = res?.map((item) => ({ ...item, tags: [{ ...tagData[i], tagName: tagData[i].sourceTag }] })) || [];
      const nextData = await sequenceRequest(requests, tagData, i + 1);
      return (currentData).concat(nextData);
    });
  }
  return data;
}
export async function requestPreviewData(tagData: any[], request: (item: any) => Promise<any>): Promise<any[]> {
  const requestStack: Array<() => Promise<any[]>> = tagData.map((data) => {
    console.log('requestPreviewData oneData..requestStack', data);
    return () => request(data);// publishVersionApi.comparePreviewTag(publishVersionId, data);
  });
  const tableData = await sequenceRequest(requestStack, tagData, 0);
  return tableData;
}
export function transformFilter(fields: Map<string, any>) {
  console.log('fields');
  const systemFields = {};
  for (const [code, field] of fields) {
    const value = toJS(field.value);
    if (value === undefined || value === null || value === '') {
      // eslint-disable-next-line no-continue
      continue;
    }
    set(systemFields, code, value);
  }
  set(systemFields, 'otherArgs', {});
  return systemFields as any;
}
interface FilterObject {
  contents: string
  assigneeId: string[]
  issueTypeId: string[]
  priorityId: string[]
  statusId: string[]
}
const FilterMapIssue = {
  contents: 'summary',
  assigneeId: 'assigneeId',
  issueTypeId: 'issueTypeId',
  priorityId: 'priorityVO.id',
  statusId: 'statusVO.id',
};
function isPassUnAssigneeFilter(fieldValue: string[], issueValue: string): boolean {
  return fieldValue.includes('0') && isEmpty(issueValue);
}
export function issuesFilter<T extends Partial<FilterObject>>(issues: Array<Issue>, filters: T) { // 'issueTypeId', 'priorityId', 'statusId', 'assigneeId'
  const keys = Object.keys(omit(filters, 'contents', 'otherArgs'));
  console.log('issues', issues);
  return issues.filter((issue) => {
    for (let index = 0; index < keys.length; index += 1) {
      const issueValue = get(issue, FilterMapIssue[keys[index] as keyof typeof FilterMapIssue], '');
      const filterFieldValue = get(filters, keys[index]);
      console.log('issueValue', issueValue, filterFieldValue, isPassUnAssigneeFilter(filterFieldValue, issueValue), filterFieldValue.includes(String(issueValue)));
      if (!(isPassUnAssigneeFilter(filterFieldValue, issueValue) || filterFieldValue.includes(String(issueValue)))) {
        return false;
      }
    }
    if (!isEmpty(filters.contents)) {
      return get(issue, FilterMapIssue.contents).toLowerCase().indexOf(String(filters.contents).toLowerCase()) !== -1;
    }
    return true;
  });
}
