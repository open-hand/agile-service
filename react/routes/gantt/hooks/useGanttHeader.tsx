import React, { useCallback, useState, useRef } from 'react';
import {
  useCreation, usePersistFn, useWhyDidYouUpdate,
} from 'ahooks';

import {
  Tooltip, Icon, Button, CheckBox,
} from 'choerodon-ui/pro';
import { find, get, noop } from 'lodash';
import type { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import openCreateIssue from '@/components/create-issue';
import { SelectSprintProps } from '@/components/select/select-sprint';
import GanttStore from '../stores/store';
import { openCustomColumnManageModal } from '@/components/table-cache/column-manage/Modal';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { typeOptions } from '../components/gannt-select/SelectType';
import type { IGanttDimensionTypeValue } from '../components/gannt-select/SelectType';
import useFullScreen from '@/common/useFullScreen';
import { useGanttContext } from '../stores/context';
import useFormatMessage from '@/hooks/useFormatMessage';

interface IGanttHeaderHookConfig {
  projectId?: string
  visibleColumnCodes?: string[]
  isHasConflict?: boolean
  columnOptions?: Array<{ code: string, title: string }>
  onTypeChange?: (type: IGanttDimensionTypeValue) => void
  onRefresh?: () => void
  onClickPersonalFilter?: () => void
  menuType?: 'project' | 'org' | 'workbench'
  store: GanttStore
}
type IGanttHeaderHookConfigKey = keyof IGanttHeaderHookConfig
const typeValues = typeOptions.map((t) => t.value);

function useGanttHeader(config: IGanttHeaderHookConfig) {
  const { projectId, store, menuType } = config;
  const { fullButtonProps } = useGanttContext();
  const formatMessage = useFormatMessage();
  const { sprintIds } = store;
  const [isHasConflict, setIsHasConflict] = useState(false);
  const configMaps = useCreation(() => new Map<IGanttHeaderHookConfigKey, any>(), []);
  const fullDomRef = useRef<any>(null);
  // const [isFullScreen, { toggleFull: toggleFullScreen }] = useFullscreen(fullDomRef, {});
  const [isFullScreen, toggleFullScreen] = useFullScreen(() => fullDomRef.current || document.body, () => { }, 'c7n-gantt-fullScreen');
  const [processType, setProcessType] = useState<'task' | 'workTime'>('task');
  const [type, setType] = useState<IGanttDimensionTypeValue>(localPageCacheStore.project(projectId).getItem('gantt.search.type') ?? typeValues[0]);
  const setConfig = useCallback((configItemKey: IGanttHeaderHookConfigKey, value?: any) => {
    if (configItemKey === 'isHasConflict') {
      setIsHasConflict(value);
    } else {
      configMaps.set(configItemKey, value);
    }
  }, [configMaps]);
  const getConfig = useCallback((configItemKey: IGanttHeaderHookConfigKey, defaultValue = noop) => get(config, configItemKey) ?? configMaps.get(configItemKey) ?? defaultValue, [config, configMaps]);
  const handleTypeChange = usePersistFn((newType) => {
    getConfig('onTypeChange')(newType);
    setType(newType);
    localPageCacheStore.project(projectId).setItem('gantt.search.type', newType);
  });
  const handleSprintChange = useCallback((value: string[]) => {
    store.setSprintIds(value);
  }, [store]);
  const afterSprintLoad = useCallback((sprints) => {
    if (!store.sprintIds) {
      const cachedSprintId = localPageCacheStore.project(projectId).getItem('gantt.search.sprints');
      if (cachedSprintId) {
        store.setSprintIds(cachedSprintId);
      } else {
        const currentSprint = find(sprints, { statusCode: 'started' });
        if (currentSprint) {
          store.setSprintIds([currentSprint.sprintId]);
        } else {
          store.setSprintIds([sprints[0]?.sprintId || '0']);
        }
      }
    }
  }, [projectId, store]);
  const sprintComponentsProps = useCreation(() => ({
    key: `SelectSprint-${projectId}`,
    flat: true,
    value: sprintIds,
    statusList: [],
    projectId,
    placeholder: '冲刺',
    multiple: true,
    onChange: handleSprintChange,
    clearButton: false,
    afterLoad: afterSprintLoad,
    hasUnassign: true,
    maxTagTextLength: 12,
    style: { marginRight: 16 },
    maxTagCount: 2,
    searchable: false,
    selectAllButton: false,
  }) as SelectSprintProps, [projectId, sprintIds, handleSprintChange]);
  const processTypeComponentProps = {
    value: processType,
    onChange: setProcessType,
    clearButton: false,
    style: { marginRight: 8, width: 100 },
  } as Partial<SelectProps>;
  const typeComponentProps = {
    value: type,
    isHasConflict,
    onChange: handleTypeChange,
    style: { marginRight: 16 },
  } as Partial<SelectProps>;
  const headerComponentProps = useCreation(() => {
    const itemMap = {
      create: {
        name: formatMessage({ id: 'agile.common.create.issue' }),
        icon: 'playlist_add',
        display: true,
        handler: () => {
          openCreateIssue({
            defaultValues: { sprint: sprintIds?.length === 1 ? sprintIds.filter((item) => item !== '0')[0] : undefined },
            onCreate: () => getConfig('onRefresh')(),
          });
        },
      },
      personalFilter: {
        name: formatMessage({ id: 'agile.common.personal.filter' }),
        icon: 'settings-o',
        display: true,
        handler: getConfig('onClickPersonalFilter'),
      },
      columnConfig: {
        display: true,
        name: formatMessage({ id: 'agile.common.column.config' }),
        // icon: 'view_column-o',
        handler: () => {
          openCustomColumnManageModal({
            modelProps: {
              title: '设置列显示字段',
            },
            projectId,
            value: getConfig('visibleColumnCodes', []),
            options: getConfig('columnOptions', []),
            type: 'gantt',
          });
        },
        element: (
          <Button>
            <Icon
              type="view_column-o"
              style={{ fontSize: 20 }}
            />
            <span>{ formatMessage({ id: 'agile.common.column.config' })}</span>
          </Button>),
      },
      fullScreen: {
        icon: isFullScreen ? 'fullscreen_exit' : 'zoom_out_map',
        iconOnly: true,
        display: true,
        handler: () => {
          toggleFullScreen();
        },
        tooltipsConfig: {
          title: isFullScreen ? '退出全屏' : '全屏',
        },
        ...fullButtonProps,
      },
      refresh: {
        icon: 'refresh',
        // funcType: 'flat',
        handler: getConfig('onRefresh'),
      },
    };
    if (menuType === 'project') {
      return {
        showClassName: false,
        items: ['create', 'personalFilter', 'columnConfig', 'fullScreen', 'refresh'].map((key: keyof typeof itemMap) => itemMap[key]),
      };
    }
    if (menuType === 'workbench') {
      return {
        showClassName: false,
        items: ['columnConfig', 'personalFilter', 'fullScreen'].map((key: keyof typeof itemMap) => itemMap[key]),

      };
    }
    return {
      showClassName: false,
      items: ['columnConfig', 'personalFilter'].map((key: keyof typeof itemMap) => itemMap[key]),
    };
  }, [menuType, getConfig]);
  return [{
    type, processType, setConfig, fullDomRef, isFullScreen,
  }, {
    sprintComponentsProps, processTypeComponentProps, typeComponentProps, headerComponentProps,
  }] as const;
}
type F = ReturnType<typeof useGanttHeader>
type IGanntHeaderHookData = F[0]
export type { IGanntHeaderHookData };
export default useGanttHeader;
