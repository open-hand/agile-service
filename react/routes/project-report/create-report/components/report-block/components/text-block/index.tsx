import React from 'react';
import WYSIWYGViewer from '@/components/WYSIWYGViewer';
import { IReportBlock } from '@/routes/project-report/create-report/store';

interface Props {
  data: IReportBlock
}
const TextBlock: React.FC<Props> = ({ data: { data } }) => <WYSIWYGViewer data={data} />;

export default TextBlock;
