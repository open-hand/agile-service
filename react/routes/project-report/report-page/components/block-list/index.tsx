import React from 'react';
import { useProjectReportContext } from '../../context';
import BlockList from './BlockList';
import BlockListPreview from './Preview';

interface Props {
  preview?: boolean
}
const BlockListIndex: React.FC<Props> = ({
  preview: forcePreview,
}) => {
  const { preview } = useProjectReportContext();
  const isPreview = forcePreview || preview;
  return isPreview ? <BlockListPreview /> : <BlockList />;
};
export default BlockListIndex;
