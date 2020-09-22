import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
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
  const [, props] = useBurnDownReport(config, onFinish);
  return (
    <div>
      <BurnDown {...props} animation={false} />
    </div>
  );
};
export default observer(BurnDownComponent);
