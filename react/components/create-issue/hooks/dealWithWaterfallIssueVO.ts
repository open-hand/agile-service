import { omit } from 'lodash';
import { IssueValueHook } from '../interface';
import { WATERFALL_TYPE_CODES } from '@/constants/TYPE_CODE';

const dealWithWaterfallIssueVO: IssueValueHook = (values, data) => {
  const isWaterfallCurrentIssue = WATERFALL_TYPE_CODES.includes(values.typeCode);
  if (isWaterfallCurrentIssue || (data.parentIssueId?.typeCode && WATERFALL_TYPE_CODES.includes(data.parentIssueId.typeCode))) {
    return {
      ...omit(values, ['parentId', 'deliverableData', 'parentIssueId', 'relateIssueId']),
      waterfallIssueVO: {
        wfIssueRelId: isWaterfallCurrentIssue ? undefined : data.parentIssueId.issueId,
        parentId: isWaterfallCurrentIssue ? values.parentId || undefined : undefined,
        wfDeliverableVOS: values.typeCode === 'milestone' ? values.deliverableData : undefined,
      },
    };
  }
  return {
    ...omit(values, ['parentId', 'deliverableData']),
  };
};
export default dealWithWaterfallIssueVO;
