import React from 'react';
import WYSIWYGViewer from '@/components/WYSIWYGViewer';
import { IReportTextBlock } from '@/routes/project-report/create-report/store';

interface Props {
  data: IReportTextBlock
}
const TextBlock: React.FC<Props> = ({ data: { data } }) => (
  <div style={{ padding: '10px 26px' }}>
    <WYSIWYGViewer data={data} />
  </div>
);

export default TextBlock;
