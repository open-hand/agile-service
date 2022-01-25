import React from 'react';
import useIsInProgram from '@/hooks/useIsInProgram';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import BacklogHome from './BacklogHome';
import useIsProgram from '@/hooks/useIsProgram';

const BacklogHomeHoc = (props) => {
  const {
    loading, ...restData
  } = useIsInProgram({ menuType: 'project' });
  const { isAgileProgram } = useIsProgram();
  BacklogStore.setIsInProgramData({
    loading,
    ...restData,
  });
  return !loading && <BacklogHome {...restData} {...props} isAgileProgram={isAgileProgram} />;
};

export default BacklogHomeHoc;
