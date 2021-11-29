import { usePersistFn } from 'ahooks';
import { Select } from 'choerodon-ui/pro';
import React, { RefObject } from 'react';

export default function useSelectFooter(ref: RefObject<Select>, footer: React.ReactNode) {
  const menu = React.Children.toArray(ref?.current?.getPopupContent()) ?? [];
  const popupContent = usePersistFn(() => [
    ...menu,
    footer,
  ]);
  return footer ? {
    popupContent,
  } : {};
}
