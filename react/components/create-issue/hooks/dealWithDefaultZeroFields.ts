import { IssueValueHook } from '../interface';
// 有些字段不能传null，没值需要传0
const defaultZeroFields = [
  'priorityId',
  'sprintId',
  'epicId',
  'piId',
  'parentIssueId',
  'relateIssueId',
];

const dealWithDefaultZeroFields: IssueValueHook = (values) => {
  defaultZeroFields.forEach((key) => {
    // eslint-disable-next-line no-param-reassign
    values[key] = values[key] ?? 0;
  });
  return values;
};
export default dealWithDefaultZeroFields;
