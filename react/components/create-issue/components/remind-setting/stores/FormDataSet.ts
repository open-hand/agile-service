import { DataSet } from 'choerodon-ui/pro';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';

interface Props {
  issueId: string,
  typeDs: DataSet,
  unitDs: DataSet,
}

export default ({
  issueId, typeDs, unitDs,
}: Props): DataSetProps => ({
  autoQuery: false,
  autoCreate: true,
  fields: [
    {
      name: 'timeType', type: FieldType.string, label: '提醒时间', options: typeDs,
    },
    { name: 'relativeTimeUnit', type: FieldType.string, options: unitDs },
    { name: 'relativeTime', type: FieldType.number },
    { name: 'customTime', type: FieldType.string },
    { name: 'userType', type: FieldType.object, label: '通知对象' },
  ],
});
