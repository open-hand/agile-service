import React, { useImperativeHandle, useCallback, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import EpicReport from '@/components/charts/epic-report';
import EpicReportSearch from '@/components/charts/epic-report/search';
import useEpicReport from '@/components/charts/epic-report/useEpicReport';
import { getProjectId } from '@/utils/common';
import { IReportChartBlock, EpicReportSearchVO } from '@/routes/project-report/report-page/store';
import { ChartRefProps } from '../..';

export const transformEpicReportSearch = (searchVO: EpicReportSearchVO) => {
  if (!searchVO) {
    return undefined;
  }
  return ({
    unit: searchVO.type,
    epicId: searchVO.epicId,
  });
};
interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
}
const EpicReportComponent:React.FC<Props> = ({ innerRef, data }) => {
  const config = useMemo(() => transformEpicReportSearch(data?.chartSearchVO as EpicReportSearchVO), [data?.chartSearchVO]);
  const [props, searchProps] = useEpicReport(config);
  const { unit, epicId } = searchProps;
  const handleSubmit = useCallback(async (): Promise<EpicReportSearchVO> => ({
    epicId,
    type: unit,
    projectId: getProjectId(),
  }),
  [unit, epicId]);

  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <EpicReportSearch {...searchProps} />
      <EpicReport {...props} />
    </div>
  );
};

export default observer(EpicReportComponent);
