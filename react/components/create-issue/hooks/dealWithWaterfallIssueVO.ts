import { omit } from 'lodash';
import { IssueValueHook } from '../interface';
import { WATERFALL_TYPE_CODES } from '@/constants/TYPE_CODE';

const dealWithWaterfallIssueVO: IssueValueHook = (values) => {
  if (WATERFALL_TYPE_CODES.includes(values.typeCode)) {
    return {
      ...omit(values, ['parentId', 'deliverableData']),
      waterfallIssueVO: {
        parentId: values.parentId || undefined,
        wfDeliverableVOS: values.typeCode === 'milestone' ? values.deliverableData : undefined,
      },
    };
  }
  return {
    ...omit(values, ['parentId', 'deliverableData']),
  };
  return values;
};
export default dealWithWaterfallIssueVO;
