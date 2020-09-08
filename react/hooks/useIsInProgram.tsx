import React, { useEffect, useState, useCallback } from 'react';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import { commonApi } from '@/api';
import { stores } from '@choerodon/boot';

const { AppState } = stores;
const isDEV = process.env.NODE_ENV === 'development';
// @ts-ignore
const HAS_AGILE_PRO = C7NHasModule('@choerodon/agile-pro');
const shouldRequest = isDEV || HAS_AGILE_PRO;
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

const useIsInProgram = (): ChildrenProps => {
  const [isInProgram, setIsInProgram] = useState<boolean>(false);
  const [program, setProgram] = useState<object | boolean>(false);
  const [isShowFeature, setIsShowFeature] = useState<boolean>(false);
  const [artInfo, setArtInfo] = useState<object | boolean>(false);
  const [loading, setLoading] = useState<boolean>(true);

  const type = AppState.currentMenuType.category === 'PROGRAM' ? 'program' : 'agile';
  const isProgram = type === 'program';

  const refresh = useCallback(async () => {
    if (!isProgram) {
      setLoading(true);
      const projectProgram = shouldRequest ? await commonApi.getProjectsInProgram() : Promise.resolve(false);
      const hasProgram = Boolean(projectProgram);
      let art = false;
      let showFeature = false;
      if (hasProgram) {
        art = await commonApi.getIsShowFeature();
        showFeature = Boolean(art);
      }
      batchedUpdates(() => {
        setIsInProgram(hasProgram);
        setProgram(projectProgram);
        setArtInfo(art);
        setIsShowFeature(showFeature);
        setLoading(false);
      });
    }
  }, [isProgram]);

  useEffect(() => {
    refresh();
  }, [refresh]);

  return {
    isInProgram, program, isShowFeature, artInfo, loading, refresh,
  };
};

const IsInProgram: React.FC<Props> = ({ children }) => {
  const data = useIsInProgram();
  return children(data);
};

export { IsInProgram };
export default useIsInProgram;
