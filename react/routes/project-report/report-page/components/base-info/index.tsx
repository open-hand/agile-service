import React from 'react';
import { useProjectReportContext } from '../../context';
import BaseInfo from './BaseInfo';
import BaseInfoPreview from './Preview';

interface Props {
  preview?: boolean
}
const BaseInfoIndex: React.FC<Props> = ({
  preview: forcePreview,
}) => {
  const { preview } = useProjectReportContext();
  const isPreview = forcePreview || preview;
  return isPreview ? <BaseInfoPreview /> : <BaseInfo />;
};
export default BaseInfoIndex;
