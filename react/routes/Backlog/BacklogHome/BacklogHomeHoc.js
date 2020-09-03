import React from 'react';
import useIsInProgram from '@/hooks/useIsInProgram';
import BacklogHome from './BacklogHome';

const BacklogHomeHoc = (props) => {
  const { loading, ...restData } = useIsInProgram();
  return !loading && <BacklogHome {...restData} {...props} />;
};

export default BacklogHomeHoc;
