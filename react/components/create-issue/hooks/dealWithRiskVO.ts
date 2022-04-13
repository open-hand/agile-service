import { omit, pick } from 'lodash';
import { IssueValueHook } from '../interface';

const fields = ['riskCategory', 'riskInfluence', 'riskProbability', 'riskProximity', 'copingStrategy',
  'estimatedResolutionDate', 'actualResolutionDate', 'relatedParties', 'discoveryDate', 'sourceIssueIds'];

const dealWithWaterfallIssueVO: IssueValueHook = (values) => {
  if (values.typeCode === 'risk') {
    return {
      ...omit(values, fields),
      riskVO: {
        ...pick(values, ['copingStrategy', 'estimatedResolutionDate', 'actualResolutionDate', 'discoveryDate', 'sourceIssueIds']),
        categoryId: values.riskCategory || undefined,
        influenceId: values.riskInfluence || undefined,
        probabilityId: values.riskProbability || undefined,
        proximityId: values.riskProximity || undefined,
        relatedPartyIds: values.relatedParties || undefined,
      },
    };
  }
  return {
    ...omit(values, fields),
  };
  return values;
};
export default dealWithWaterfallIssueVO;
