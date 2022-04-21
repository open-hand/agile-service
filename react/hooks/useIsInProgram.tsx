import React, { useRef } from 'react';
import { stores } from '@choerodon/boot';
import { UseQueryOptions } from 'react-query';
import { usePersistFn } from 'ahooks';
import { omit } from 'lodash';
import { AppStateProps } from '@/common/types';
import useIsProgram from './useIsProgram';
import useParentProgram from './data/useParentProgram';
import useParentArtDoing from './data/useParentArtDoing';
import useIsAgile from '@/hooks/useIsAgile';
import useIsWaterfall from '@/hooks/useIsWaterfall';
import { IHookCategoryCodesConfig } from './useCategoryCodes';

const { AppState }: { AppState: AppStateProps } = stores;
// @ts-ignore
const HAS_AGILE_PRO = C7NHasModule('@choerodon/agile-pro');
// const shouldRequest = HAS_AGILE_PRO;
const isDEV = process.env.NODE_ENV === 'development';
const shouldRequest = isDEV || HAS_AGILE_PRO;

interface ChildrenProps {
  isInProgram: boolean,
  program: object | boolean,
  isShowFeature: boolean,
  artInfo: object | boolean,
  loading: boolean,
  refresh: () => Promise<void>,
}
interface Props extends useIsInProgramConfig {
  projectId?: string
  children: (data: ChildrenProps) => React.ReactElement,
}
interface useIsInProgramConfig extends IHookCategoryCodesConfig {
  projectId?: string
  menuType?: 'project' | 'org' | 'program'
}
const useIsInProgram = (config?: useIsInProgramConfig, options?: UseQueryOptions<any>): ChildrenProps => {
  const { projectId, menuType } = config ?? {};
  // 非当前项目 下列判断都是无效结果 临时传入 categories 配置
  const { isProgram } = useIsProgram(config);
  const { isAgile } = useIsAgile(config);
  const { isWaterfall } = useIsWaterfall(config);

  const isProject = menuType ? menuType === 'project' : AppState.currentMenuType.type === 'project';
  const isFirstMount = useRef<boolean>(shouldRequest && isProject && (!isProgram || isAgile));
  const {
    data: program, isLoading: loading1, refetch: refresh1,
  } = useParentProgram({ projectId }, {
    enabled: shouldRequest && isProject && (!isProgram || isAgile) && options?.enabled && !isWaterfall,
    onSuccess: () => {
      isFirstMount.current = false;
    },
    onError: () => {
      isFirstMount.current = false;
    },
    ...omit(options, 'enabled'),
  });
  const isInProgram = Boolean(program);
  const { data: parentArtDoing, isLoading: loading2, refetch: refresh2 } = useParentArtDoing({ projectId }, {
    enabled: shouldRequest && isInProgram && options?.enabled,
    ...omit(options, 'enabled'),
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

const IsInProgram: React.FC<Props> = ({ children, ...otherConfig }) => {
  const data = useIsInProgram(otherConfig);
  return children(data);
};

export { IsInProgram };
export default useIsInProgram;
