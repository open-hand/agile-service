import { omit } from 'lodash';
import { IssueValueHook } from '../interface';

const dealWithSubBug: IssueValueHook = (values) => {
  if (values.typeCode === 'bug' && values.parentIssueId) {
    return {
      ...omit(values, 'parentIssueId'),
      relateIssueId: values.parentIssueId,
    };
  }
  return values;
};
export default dealWithSubBug;
