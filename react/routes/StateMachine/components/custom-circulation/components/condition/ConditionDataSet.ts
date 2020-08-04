import { DataSet } from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';

const memberOptionDataSet = new DataSet({
  data: [
    { code: 'owner', name: '项目所有者' },
    { code: 'member', name: '项目成员' },
    { code: 'assigners', name: '被指定人' },
  ],
  fields: [
    { 
      name: 'code',
      type: 'string' as FieldType, 
    },
    {
      name: 'name',
      type: 'string' as FieldType,
    },
  ],
});

const ConditionDataSet: DataSetProps = {
  autoCreate: true,
  fields: [
    {
      name: 'member',
      label: '成员',
      type: 'array' as FieldType,
      textField: 'name',
      valueField: 'code',
      options: memberOptionDataSet,
      multiple: true,
      required: true,
    },
    {
      name: 'assigners',
      label: '指定人',
      type: 'array' as FieldType,
      required: true,
      multiple: true,
      textField: 'realName',
      valueField: 'id',
    },
    {
      name: 'needCompleted',
      label: '任务项子级需全部到达已解决状态',
      type: 'boolean' as FieldType,
    },
  ],
};

export default ConditionDataSet;
