import React, { useEffect, useState, useCallback } from 'react';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import { commonApi } from '@/api';
import { stores } from '@choerodon/boot';
import { AppStateProps } from '@/common/types';
import useCategoryCodes from './useCategoryCodes';
import useIsProgram from './useIsProgram';

const { AppState }: { AppState: AppStateProps } = stores;
const isDEV = process.env.NODE_ENV === 'development';
// @ts-ignore
const HAS_AGILE_PRO = C7NHasModule('@choerodon/agile-pro');
const shouldRequest = isDEV || HAS_AGILE_PRO;
interface ChildrenProps {
  isInProgram: boolean,
  isShowFeature: boolean,
  artInfo: object | boolean,
  loading: boolean,
  refresh: () => Promise<void>,
}
interface Props {
  children: (data: ChildrenProps) => React.ReactElement,
}

const useIsInProgram = (): ChildrenProps => {
  const [isShowFeature, setIsShowFeature] = useState<boolean>(false);
  const [artInfo, setArtInfo] = useState<object | boolean>(false);
  const [loading, setLoading] = useState<boolean>(true);
  const { isProgram } = useIsProgram();
  const isProject = AppState.currentMenuType.type === 'project';
  const codes = useCategoryCodes();
  const isInProgram = codes.includes('N_PROGRAM_PROJECT');
  const refresh = useCallback(async () => {
    if (!isProgram) {
      setLoading(true);
      let art = false;
      let showFeature = false;
      if (shouldRequest && isProject && isInProgram) {
        art = await commonApi.getIsShowFeature();
        showFeature = Boolean(art);
      }
      batchedUpdates(() => {
        setArtInfo(art);
        setIsShowFeature(showFeature);
        setLoading(false);
      });
    } else {
      setLoading(false);
    }
  }, [isInProgram, isProgram, isProject]);

  useEffect(() => {
    refresh();
  }, [refresh]);

  return {
    isInProgram, isShowFeature, artInfo, loading, refresh,
  };
};

const IsInProgram: React.FC<Props> = ({ children }) => {
  const data = useIsInProgram();
  return children(data);
};

export { IsInProgram };
export default useIsInProgram;
