import React from 'react';
import useIsInProgram from '@/hooks/useIsInProgram';
import { useDetail } from '@/components/detail-container';
import ReleaseHome from './ReleaseHome';

function ReleaseHomeHoc() {
  const { loading, ...restData } = useIsInProgram();
  const [detailProps] = useDetail();
  return !loading && <ReleaseHome {...restData} detailProps={detailProps} />;
}
export default ReleaseHomeHoc;
