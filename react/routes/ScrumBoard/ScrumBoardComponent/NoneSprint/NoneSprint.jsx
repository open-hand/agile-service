/* eslint-disable react/self-closing-comp,jsx-a11y/accessible-emoji */
import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { EmptyPage } from '@choerodon/components';
import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';
import EmptyScrumboard from './emptyScrumboard.svg';
import useFormatMessage from '@/hooks/useFormatMessage';

const NoneSprint = ({ doingSprintExist, filterItems, hasSetFilter }) => {
  let tipTitle = 'empty.description.prefix.sprint';
  const formatMessage = useFormatMessage();

  const { sprint } = filterItems;
  if ((doingSprintExist || sprint) && Object.keys(filterItems).length === 1) {
    tipTitle = 'empty.description.prefix.plan';
  } else if (hasSetFilter) {
    tipTitle = 'empty.description.prefix.filter';
  }

  const handleLinkToBacklog = useCallback(() => {
    to(LINK_URL.workListBacklog);
  }, []);

  return (
    <>
      <EmptyPage
        image={EmptyScrumboard}
        description={formatMessage({ id: 'agile.scrumBoard.empty.description' }, {
          prefix: formatMessage({ id: `agile.scrumBoard.${tipTitle}` }),
          button: (
            <EmptyPage.Button style={{ color: '#5365EA' }} onClick={handleLinkToBacklog}>
              {formatMessage({ id: 'agile.backlog.route' })}
            </EmptyPage.Button>),
          s1: (chunk) => (!doingSprintExist ? chunk : ''),
          s2: (chunk) => (doingSprintExist ? chunk : ''),
        })}
      >
      </EmptyPage>
    </>
  );
};

export default observer(NoneSprint);
