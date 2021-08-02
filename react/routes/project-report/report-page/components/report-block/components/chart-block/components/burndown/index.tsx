import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import BurnDownSearch from '@/components/charts/burn-down/search';
import BurnDown from '@/components/charts/burn-down';
import useBurnDownReport from '@/components/charts/burn-down/useBurnDownReport';
import { BurnDownSearchVO } from '@/routes/project-report/report-page/store';
import { transformBurnDownSearch } from '../../../../../add-chart/components/burndown';

interface Props {
  filter: BurnDownSearchVO
  onFinish?: Function
}
const BurnDownComponent: React.FC<Props> = ({ filter, onFinish }) => {
  const config = useMemo(() => transformBurnDownSearch(filter), [filter]);
  const [searchProps, props] = useBurnDownReport(config, onFinish);
  return (
    <div>
      <div style={{ display: 'none' }}>
        <BurnDownSearch {...searchProps} />
      </div>
      <BurnDown
        {...props}
        option={{
          animation: false,
          grid: {
            top: 60,
            bottom: 0,
            left: 0,
            right: 30,
            containLabel: true,
          },
        }}
      />
    </div>
  );
};
export default observer(BurnDownComponent);
