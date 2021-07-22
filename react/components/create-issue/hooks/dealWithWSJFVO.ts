import { IssueValueHook } from '../interface';

const dealWithFeatureVO: IssueValueHook = (values, data) => {
  const {
    userBusinessValue,
    timeCriticality,
    rrOeValue,
    jobSize,
  } = data;
  return {
    ...values,
    wsjfVO: {
      userBusinessValue,
      timeCriticality,
      rrOeValue,
      jobSize,
    },
  };
};
export default dealWithFeatureVO;
