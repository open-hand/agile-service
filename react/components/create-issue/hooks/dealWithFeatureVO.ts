import { omit } from 'lodash';
import { IssueValueHook } from '../interface';

const dealWithFeatureVO: IssueValueHook = (values) => {
  const {
    benfitHypothesis,
    acceptanceCritera,
    featureType,
  } = values;
  return {
    ...omit(values,
      ['benfitHypothesis',
        'acceptanceCritera',
        'featureType',
      ]),
    featureVO: {
      benfitHypothesis,
      acceptanceCritera,
      featureType,
    },
  };
};
export default dealWithFeatureVO;
