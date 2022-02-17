import React from 'react';
import { has as hasInject, mount } from '@choerodon/inject';
import { SELECT_PARENT } from '@/constants/WATERFALL_INJECT';

const SelectParentWaterfall = (props: any) => {
  if (hasInject(SELECT_PARENT)) {
    return mount(SELECT_PARENT, props);
  }
  return null;
};

export default SelectParentWaterfall;
