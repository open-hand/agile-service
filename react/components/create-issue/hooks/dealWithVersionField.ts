import { omit } from 'lodash';
import { IssueValueHook } from '../interface';

const dealWithVersionField: IssueValueHook = (values) => ({
  ...omit(values,
    ['fixVersion',
      'influenceVersion',
    ]),
  versionIssueRelVOList: [
    ...(values.fixVersion ?? []).map((versionId: string) => ({ versionId, relationType: 'fix' })),
    ...(values.influenceVersion ?? []).map((versionId: string) => ({ versionId, relationType: 'influence' })),
  ],
});
export default dealWithVersionField;
