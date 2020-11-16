import React from 'react';
import useIsInProgram from '@/hooks/useIsInProgram';
import ReleaseHome from './ReleaseHome';

function ReleaseHomeHoc() {
  const { loading, ...restData } = useIsInProgram();
  return !loading && <ReleaseHome {...restData} />;
}
export default ReleaseHomeHoc;
