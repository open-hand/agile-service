import React from 'react';
import WYSIWYGViewer from '@/components/WYSIWYGViewer';
import { IReportTextBlock } from '@/routes/project-report/report-page/store';

interface Props {
  data: IReportTextBlock
}
const TextBlock: React.FC<Props> = ({ data: { content } }) => (
  <div style={{ padding: '10px 26px' }}>
    <WYSIWYGViewer data={content} />
  </div>
);

export default TextBlock;
