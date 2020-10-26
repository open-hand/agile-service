import { fieldApi } from '@/api';

export async function checkCanQuickCreate(typeCode) {
  const param = {
    schemeCode: 'agile_issue',
    context: typeCode,
    pageCode: 'agile_issue_create',
  };
  const whiteList = ['summary', 'status', 'issueType', 'priority', 'epicName'];
  const fields = await fieldApi.getFields(param);
  if (fields.some((field) => !whiteList.includes(field.fieldCode) && field.required)) {
    return false;
  }
  return true;
}
