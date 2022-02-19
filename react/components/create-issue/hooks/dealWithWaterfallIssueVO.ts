import { omit } from 'lodash';
import { IssueValueHook } from '../interface';
import { WATERFALL_TYPE_CODES } from '@/constants/TYPE_CODE';

const dealWithWaterfallIssueVO: IssueValueHook = (values) => {
  if (values.parentId) {
    if (WATERFALL_TYPE_CODES.includes(values.typeCode)) {
      return {
        ...omit(values, ['parentId']),
        waterfallIssueVO: {
          parentId: values.parentId,
        },
      };
    }
    return {
      ...omit(values, ['parentId']),
    };
  }
  return values;
};
export default dealWithWaterfallIssueVO;
