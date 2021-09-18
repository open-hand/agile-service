import React, { useRef } from 'react';
import { stores } from '@choerodon/boot';
import { useMount, usePersistFn } from 'ahooks';
import { AppStateProps } from '@/common/types';
import useIsProgram from './useIsProgram';
import useParentProgram from './data/useParentProgram';
import useParentArtDoing from './data/useParentArtDoing';
import useIsProgramProject from '@/hooks/useIsProgramProject';

const { AppState }: { AppState: AppStateProps } = stores;
// @ts-ignore
const HAS_AGILE_PRO = C7NHasModule('@choerodon/agile-pro');
const shouldRequest = HAS_AGILE_PRO;
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
interface useIsInProgramConfig {
  projectId?: string
}
const useIsInProgram = (config?: useIsInProgramConfig): ChildrenProps => {
  const { projectId } = config ?? {};
  const { isProgram } = useIsProgram();
  const isProject = AppState.currentMenuType.type === 'project';
  const isFirstMount = useRef<boolean>(shouldRequest && isProject && !isProgram);
  const { data: program, isLoading: loading1, refetch: refresh1 } = useParentProgram({ projectId }, {
    enabled: shouldRequest && isProject && !isProgram,
    onSuccess: () => {
      isFirstMount.current = false;
    },
    onError: () => {
      isFirstMount.current = false;
    },
  });
  const isInProgram = Boolean(program);
  const { data: parentArtDoing, isLoading: loading2, refetch: refresh2 } = useParentArtDoing({ projectId }, {
    enabled: shouldRequest && isInProgram,
  });

  const refresh = usePersistFn(async () => {
    await refresh1();
    await refresh2();
  });
  return {
    isInProgram,
    program,
    isShowFeature: isInProgram && Boolean(parentArtDoing),
    artInfo: parentArtDoing,
    loading: isFirstMount.current || loading1 || loading2,
    refresh,
  };
};

const IsInProgram: React.FC<Props> = ({ children }) => {
  const data = useIsInProgram();
  return children(data);
};

export { IsInProgram };
export default useIsInProgram;
