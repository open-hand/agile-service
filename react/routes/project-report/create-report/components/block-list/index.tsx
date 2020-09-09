import React from 'react';
import { useProjectReportContext } from '../../context';
import ReportBlock from '../report-block';

const BlockList: React.FC = () => {
  const { store } = useProjectReportContext();
  return (
    <>
      {store.blockList.map((block) => <ReportBlock key={block.id} data={block} />)}
    </>
  );
};
export default BlockList;
