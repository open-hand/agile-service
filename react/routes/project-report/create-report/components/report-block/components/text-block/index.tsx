import React from 'react';
import WYSIWYGViewer from '@/components/WYSIWYGViewer';
import { IReportTextBlock } from '@/routes/project-report/create-report/store';

interface Props {
  data: IReportTextBlock
}
const TextBlock: React.FC<Props> = ({ data: { data } }) => <WYSIWYGViewer data={data} />;

export default TextBlock;
