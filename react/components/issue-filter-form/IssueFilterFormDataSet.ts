import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { AxiosRequestConfig } from 'axios';
import Field from 'choerodon-ui/pro/lib/data-set/Field';
import { IChosenFieldField } from '@/components/chose-field/types';

interface Props {
  fields: IChosenFieldField[],
  isInProgram?: boolean,
}
const IssueFilterFormDataSet = ({ fields, isInProgram = true }: Props): DataSetProps => ({
  autoQuery: false,
  autoCreate: true,
  paging: false,
  selection: undefined,
  fields: [
    {
      name: 'statusId',
      label: '状态',
      valueField: 'id',
      textField: 'name',
    }, {
      name: 'sprint',
      label: '冲刺',
      required: true,
      valueField: 'sprintId',
      textField: 'sprintName',
    },
    {
      name: 'issueTypeId',
      label: '问题类型',
      valueField: 'id',
      textField: 'name',
    },
    ...isInProgram ? [{
      name: 'featureId',
      label: '所属特性',
      valueField: 'issueId',
      textField: 'summary',
    }] : [{
      name: 'epicId',
      label: '所属史诗',
      valueField: 'issueId',
      textField: 'epicName',
    }], {
      name: 'priorityId',
      label: '优先级',
      valueField: 'id',
      textField: 'name',
    }, {
      name: 'labelIssueRelVOList',
      label: '标签',
      valueField: 'labelId',
      textField: 'labelName',
    }, {
      name: 'componentIssueRelVOList',
      label: '模块',
      valueField: 'componentId',
      textField: 'name',
    }, {
      name: 'version',
      label: '版本',
      valueField: 'versionId',
      textField: 'name',
    },
    ...fields.filter((field) => field.fieldType === 'member').map((filed) => ({
      name: filed.code,
      label: filed.name,
      textField: 'realName',
      valueField: 'id',
    })),
  ],

});
export default IssueFilterFormDataSet;
