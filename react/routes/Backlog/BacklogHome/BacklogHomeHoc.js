import React from 'react';
import useIsInProgram from '@/hooks/useIsInProgram';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import BacklogHome from './BacklogHome';

const BacklogHomeHoc = (props) => {
  const {
    loading, ...restData
  } = useIsInProgram();
  BacklogStore.setIsInProgramData({
    loading,
    ...restData,
  });
  return !loading && <BacklogHome {...restData} {...props} />;
};

export default BacklogHomeHoc;
