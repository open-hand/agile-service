import React, { useImperativeHandle, useCallback, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import CodeQualityVaryReport from '@/components/charts/code-quality-vary';
import CodeQualityVarySearch from '@/components/charts/code-quality-vary/search';
import { getProjectId } from '@/utils/common';
import { CodeQualityVarySearchVO, IReportChartBlock } from '@/routes/project-report/report-page/store';
import useCodeQualityVaryReport, { CodeQualityVaryConfig } from '@/components/charts/code-quality-vary/useCodeQualityVaryReport';
import { ChartRefProps } from '../..';

export const transformCodeQualityVarySearch = (searchVO: CodeQualityVarySearchVO | undefined): CodeQualityVaryConfig | undefined => {
  if (!searchVO) {
    return undefined;
  }
  return {
    projectId: searchVO.projectId,
    days: searchVO.days,
  };
};
interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
  projectId?: string
}
const CodeQualityVaryReportComponent:React.FC<Props> = ({ innerRef, projectId, data }) => {
  const config = useMemo(() => ({
    ...transformCodeQualityVarySearch(data?.chartSearchVO as CodeQualityVarySearchVO),
    projectId,
  }), [data?.chartSearchVO, projectId]);
  const [props, searchProps] = useCodeQualityVaryReport(config);
  const handleSubmit = useCallback(async (): Promise<CodeQualityVarySearchVO> => ({
    projectId: searchProps.projectId || getProjectId(),
    days: searchProps.days,
  }),
  [searchProps.days, searchProps.projectId]);

  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <CodeQualityVarySearch {...searchProps} />
      <CodeQualityVaryReport {...props} />
    </div>
  );
};

export default observer(CodeQualityVaryReportComponent);
