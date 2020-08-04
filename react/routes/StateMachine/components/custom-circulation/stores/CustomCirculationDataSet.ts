import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';

const CustomCirculationDataSet: DataSetProps = {
  fields: [
    {
      name: 'state',
      label: '状态',
      type: 'string' as FieldType,
    },
    {
      name: 'fieldsInfo',
      label: '状态流转附加字段信息',
      type: 'array' as FieldType,
    },
    {
      name: 'action',
      label: '自定义操作',
      type: 'string' as FieldType,
    },
  ],
  data: [
    {
      state: '待处理',
      fieldsInfo: [],
    },
    {
      state: '处理中',
      fieldsInfo: [],
    },
  ],
};

export default CustomCirculationDataSet;
