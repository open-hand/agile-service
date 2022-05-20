import React, {
  useCallback, useState, useEffect, useMemo,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Form, Button, DataSet,
} from 'choerodon-ui/pro';
import { Permission } from '@choerodon/boot';
import { map } from 'lodash';
import SelectFilter from './select-filter';
import useFormatMessage from '@/hooks/useFormatMessage';
import { boardApi } from '@/api';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';

const ChooseFilter = () => {
  const formatMessage = useFormatMessage('agile.scrumBoard');
  const boardId = ScrumBoardStore.getSelectedBoard;
  const [hasGetted, setHasGetted] = useState<boolean>(false);
  const filterDs = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'filterIds',
    }],
    events: {
      update: async ({ name, value }: { name: string, value: string[] | null }) => {
        if (name === 'filterIds') {
          await boardApi.updateFilterSelected(boardId, value || []);
          ScrumBoardStore.axiosGetSelectedBoardFilterIds();
        }
      },
    },
  }), [boardId]);
  useEffect(() => {
    const getSelectedFilters = async () => {
      const res = await boardApi.getFilterSelected(boardId);
      filterDs.current?.init('filterIds', map(res, 'quickFilterId'));
      setHasGetted(true);
    };
    getSelectedFilters();
  }, [boardId, filterDs]);

  const handleClickBtn = useCallback(() => {
    to(LINK_URL.quickFilter);
  }, []);

  return (
    hasGetted ? (
      <Permission
        service={['choerodon.code.project.setting.issue.ps.fastsearch']}
      >
        {
        (hasPermission: boolean) => (
          <>
            <Form style={{ width: 510 }} dataSet={filterDs}>
              <SelectFilter disabled={!hasPermission} name="filterIds" boardId={boardId} defaultSelectedIds={filterDs.current?.get('filterIds')} />
            </Form>
            {
              hasPermission && (
                <Button icon="playlist_add" style={{ marginTop: -4 }} onClick={handleClickBtn}>{formatMessage({ id: 'create.more.filter' })}</Button>
              )
            }
          </>
        )
      }
      </Permission>
    ) : null
  );
};

export default observer(ChooseFilter);
