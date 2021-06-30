import React from 'react';
import { observer } from 'mobx-react-lite';
import IssueFilterForm, { IIssueFilterFormProps } from '@/components/issue-filter-form';
import ChooseField, { IChoseFieldComponentProps } from '@/components/chose-field';

const Search: React.FC<{
  choseComponentProps: IChoseFieldComponentProps,
  filterComponentProps: IIssueFilterFormProps,
}> = ({ filterComponentProps, choseComponentProps }) => (
  <div>
    <IssueFilterForm {...filterComponentProps} formColumns={1}>
      <div>
        <ChooseField {...choseComponentProps} dropDownBtnProps={{ icon: 'add', style: { marginLeft: 6 } }} />
      </div>
    </IssueFilterForm>
  </div>
);

export default observer(Search);
