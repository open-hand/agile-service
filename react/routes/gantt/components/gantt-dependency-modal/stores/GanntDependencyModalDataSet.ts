import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import {
  Modal, DataSet, Form, Select, Button, Icon,
} from 'choerodon-ui/pro';

const options = ['FF', 'FS', 'SF', 'SS'];

const GanntDependencyModalDataSet = ():DataSetProps => ({
  autoCreate: true,
  selection: false,
  fields: [
    {
      name: 'relationship',
      label: '依赖关系',
      required: true,
      type: 'string' as FieldType,
      options: new DataSet({
        paging: false,
        data: options.map((item) => ({ meaning: item, value: item })),
      }),
    },
    {
      name: 'issue', label: '工作项', required: true, type: 'string' as FieldType,
    },

  ],
  transport: {
  },
});
export default GanntDependencyModalDataSet;
