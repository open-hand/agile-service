import React from 'react';
import { stores } from '@choerodon/boot';
import { AppStateProps } from '@/common/types';
import { usePersistFn } from 'ahooks';
import useIsProgram from './useIsProgram';
import useParentProgram from './data/useParentProgram';
import useParentArtDoing from './data/useParentArtDoing';

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
  const { data: program, isLoading: loading1, refetch: refresh1 } = useParentProgram({ projectId }, {
    enabled: shouldRequest && isProject && !isProgram,
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
    isShowFeature: Boolean(parentArtDoing),
    artInfo: parentArtDoing,
    loading: loading1 || loading2,
    refresh,
  };
};

const IsInProgram: React.FC<Props> = ({ children }) => {
  const data = useIsInProgram();
  return children(data);
};

export { IsInProgram };
export default useIsInProgram;
