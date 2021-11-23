import {
  DataSet,
} from 'choerodon-ui/pro';
import { FieldProps } from 'choerodon-ui/pro/lib/data-set/Field';
import { useCreation } from 'ahooks';
import IssueFilterFormDataSet from '../stores/IssueFilterFormDataSet';
import { IChosenFieldField } from '@/components/chose-field/types';

export default function useIssueFilterFormDataSet(props: { fields: IChosenFieldField[], systemFields?: FieldProps[] }) {
  return useCreation(() => new DataSet(IssueFilterFormDataSet({ fields: props.fields, systemFields: props.systemFields })), []);
}
