import { fieldApi } from '@/api';

export async function checkCanQuickCreate(typeId) {
  const param = {
    schemeCode: 'agile_issue',
    issueTypeId: typeId,
    pageCode: 'agile_issue_create',
  };
  const whiteList = ['summary', 'status', 'reporter', 'issueType', 'priority', 'epicName'];
  const fields = await fieldApi.getFields(param);
  if (fields.some((field) => !whiteList.includes(field.fieldCode) && field.required && !field.defaultValue)) {
    return false;
  }
  return true;
}
