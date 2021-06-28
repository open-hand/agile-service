import React from 'react';
import { observer } from 'mobx-react-lite';
import IssueFilterForm from '@/components/issue-filter-form';
import ChooseField from '@/components/chose-field';
import { ChartProps } from '../../Chart';

const Search: React.FC<{
  chartProps: ChartProps
}> = ({ chartProps }) => {
  const { filterComponentProps, choseComponentProps } = chartProps;
  return (
    <div>
      <IssueFilterForm {...filterComponentProps} formColumns={1}>
        <div>
          <ChooseField {...choseComponentProps} dropDownBtnProps={{ icon: 'add', style: { marginLeft: 6 } }} />
        </div>
      </IssueFilterForm>
    </div>
  );
};

export default observer(Search);
