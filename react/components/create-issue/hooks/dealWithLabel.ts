import { IssueValueHook } from '../interface';

const dealWithLabel: IssueValueHook = (values, data) => {
  const {
    labelIssueRelVOList,
  } = values;
  return {
    ...values,
    labelIssueRelVOList: labelIssueRelVOList.map((l: any) => (l.projectId ? l : {
      labelName: l.meaning,
      projectId: values.projectId,
    })),
  };
};
export default dealWithLabel;
