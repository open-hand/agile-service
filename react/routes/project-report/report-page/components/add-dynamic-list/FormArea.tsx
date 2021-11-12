import React, {
  ReactElement, useCallback, useImperativeHandle,
} from 'react';
import { observer } from 'mobx-react-lite';
import IssueFilterForm from '@/components/issue-filter-form';
import ChooseField from '@/components/chose-field';
import { useExportIssueStore } from './stores';
import { getCustomFieldFilters } from './utils';
import { ListRefProps } from '.';
import { SearchVO } from '../../store';
import './FormArea.less';

interface FormPartProps {
  title: string | ReactElement,
  children: ReactElement | ReactElement[] | null,
  btnOnClick?: (nextBtnStatusCode: 'ALL' | 'NONE') => boolean,
}
interface Props {
  innerRef: React.MutableRefObject<ListRefProps>
}
const ExportIssue: React.FC<Props> = ({ innerRef }) => {
  const {
    prefixCls,
    choseFieldStore,
    issueFilterFormDataSet,
  } = useExportIssueStore();
  const handleSubmit: ListRefProps['submit'] = useCallback(async () => {
    if (await issueFilterFormDataSet.current?.validate()) {
      const searchVO = getCustomFieldFilters(choseFieldStore.getAllChosenField, issueFilterFormDataSet.current!) as SearchVO;
      return {
        searchVO,
      };
    }
    return false;
  }, [choseFieldStore.getAllChosenField, issueFilterFormDataSet]);
  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <IssueFilterForm
        dataSet={issueFilterFormDataSet}
        chosenFields={choseFieldStore.getAllChosenField}
        onDelete={(item) => choseFieldStore.delChosenFields(item.code)}
      >
        <ChooseField store={choseFieldStore} dropDownBtnProps={{ icon: 'add' }} />
      </IssueFilterForm>
    </div>
  );
};

export default observer(ExportIssue);
