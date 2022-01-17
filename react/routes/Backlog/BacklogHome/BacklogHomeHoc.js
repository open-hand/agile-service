import React from 'react';
import useIsInProgram from '@/hooks/useIsInProgram';
import useIsProgram from '@/hooks/useIsProgram';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import BacklogHome from './BacklogHome';

const BacklogHomeHoc = (props) => {
  const {
    loading, isInProgram, isShowFeature, ...restData
  } = useIsInProgram();
  const { isProgram } = useIsProgram();
  BacklogStore.setIsInProgramData({
    loading,
    isInProgram: isProgram || isInProgram,
    isShowFeature: isProgram || isShowFeature,
    ...restData,
  });
  return !loading && <BacklogHome isInProgram={isProgram || isInProgram} isShowFeature={isProgram || isShowFeature} {...restData} {...props} />;
};

export default BacklogHomeHoc;
