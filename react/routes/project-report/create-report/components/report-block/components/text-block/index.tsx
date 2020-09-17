import React from 'react';
import WYSIWYGViewer from '@/components/WYSIWYGViewer';
import { IReportTextBlock } from '@/routes/project-report/create-report/store';

interface Props {
  data: IReportTextBlock
}
const TextBlock: React.FC<Props> = ({ data: { content } }) => (
  <div style={{ padding: '10px 26px' }}>
    <WYSIWYGViewer data={content} />
  </div>
);

export default TextBlock;
