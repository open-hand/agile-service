import React, { useImperativeHandle, useCallback, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import CodeQualityReport from '@/components/charts/code-quality';
import { getProjectId } from '@/utils/common';
import { CodeQualitySearchVO, IReportChartBlock } from '@/routes/project-report/report-page/store';
import useCodeQualityReport, { CodeQualityConfig } from '@/components/charts/code-quality/useCodeQualityReport';
import { ChartRefProps } from '../..';

export const transformCodeQualitySearch = (searchVO: CodeQualitySearchVO | undefined): CodeQualityConfig | undefined => {
  if (!searchVO) {
    return undefined;
  }
  return {
    projectId: searchVO.projectId,
  };
};
interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
  projectId?: string
}
const CodeQualityReportComponent:React.FC<Props> = ({ innerRef, projectId, data }) => {
  const config = useMemo(() => ({
    projectId,
  }), [projectId]);
  const [props, searchProps] = useCodeQualityReport(config);
  const handleSubmit = useCallback(async (): Promise<CodeQualitySearchVO> => ({
    projectId: searchProps.projectId || getProjectId(),
  }),
  [searchProps.projectId]);

  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <CodeQualityReport {...props} />
    </div>
  );
};

export default observer(CodeQualityReportComponent);
