import React from 'react';
import { useProjectReportContext } from '../../context';
import ReportBlock from '../report-block';

const BlockPreview: React.FC = () => {
  const { store } = useProjectReportContext();
  return (
    <>
      {store.blockList.map((block, index) => (
        <ReportBlock preview key={block.key} index={index} data={block} />
      ))}
    </>
  );
};
export default BlockPreview;
