import React, { useEffect, useState, useCallback } from 'react';
import { commonApi } from '@/api';
import { stores } from '@choerodon/boot';

const { AppState } = stores;

interface ChildrenProps {
  isInProgram: boolean,
  program: object | boolean,
  isShowFeature: boolean,
  artInfo: object | boolean,
  loading: boolean,
  refresh: () => Promise<void>,
}
interface Props {
  children: (data: ChildrenProps) => React.ReactElement,
}

const useIsInProgram = ():ChildrenProps => {
  const [isInProgram, setIsInProgram] = useState<boolean>(false);
  const [program, setProgram] = useState<object | boolean>(false);
  const [isShowFeature, setIsShowFeature] = useState<boolean>(false);
  const [artInfo, setArtInfo] = useState<object | boolean>(false);
  const [loading, setLoading] = useState<boolean>(false);

  const type = AppState.currentMenuType.category === 'PROGRAM' ? 'program' : 'agile';
  const isProgram = type === 'program';

  const loadIsShowFeature = useCallback(async () => {
    const art = await commonApi.getIsShowFeature();
    setArtInfo(art);
    setIsShowFeature(Boolean(art));
    setLoading(false);
    return Boolean(art);
  }, []);

  const refresh = useCallback(async () => {
    if (!isProgram) {
      setLoading(true);
      const projectProgram = await commonApi.getProjectsInProgram();
      const hasProgram = Boolean(projectProgram);
      setIsInProgram(hasProgram);
      setProgram(projectProgram);
      if (hasProgram) {
        loadIsShowFeature();
      } else {
        setIsShowFeature(false);
        setLoading(false);
      }
    } else {
      setIsInProgram(false);
    }
  }, [isProgram, loadIsShowFeature]);

  useEffect(() => {
    refresh();
  }, [refresh]);

  return {
    isInProgram, program, isShowFeature, artInfo, loading, refresh,
  };
};

const IsInProgram:React.FC<Props> = ({ children }) => {
  const data = useIsInProgram();
  return children(data);
};

export { IsInProgram };
export default useIsInProgram;
