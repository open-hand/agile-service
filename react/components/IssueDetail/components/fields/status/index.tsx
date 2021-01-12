import React, { useMemo, forwardRef } from 'react';
import { Permission } from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import { Select } from 'choerodon-ui/pro';
import STATUS from '@/constants/STATUS';
import TextEditToggle from '@/components/TextEditTogglePro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { useDetailContext } from '@/components/IssueDetail/context';
import Field from '../field';
import fieldStyles from '../index.less';

interface Props {
  statusArgs: {
    demandId: string
    statusId: number
  },
}

const Status: React.FC<{ readonly?: boolean }> = ({ readonly }) => {
  const { store } = useDetailContext();
  const { issue: { statusVO } } = store;
  const name = statusVO?.name;
  const type = statusVO?.type;
  const statusId = statusVO?.id;

  return (
    <Field label="状态">

      {
        statusId ? (
          <div
            style={{
              background: STATUS[type],
              color: '#fff',
              borderRadius: '2px',
              padding: '0 8px',
              display: 'inline-block',
              margin: '2px auto 2px 0',
            }}
          >
            {name}
          </div>
        ) : (
          <div>
            无
          </div>
        )
      }
    </Field>
  );
};

export default observer(Status);
