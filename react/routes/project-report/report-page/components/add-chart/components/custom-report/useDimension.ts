import React, { useCallback, useEffect, useState } from 'react';
import { pageConfigApi } from '@/api';
import { IField } from '@/common/types';

const useDimension = () => {
  const [dimension, setDimension] = useState<IField[]>([]);

  const getDimension = useCallback(async () => {
    const res = await pageConfigApi.load();
    const newDimension = res.content.filter((item: IField) => ['single', 'checkbox', 'multiple', 'radio', 'member', 'multiMember'].includes(item.fieldType) && !(item.contexts.length === 1 && item.contexts[0] === 'backlog'));
    setDimension(newDimension);
  }, []);

  useEffect(() => {
    getDimension();
  }, []);

  return dimension;
};

export default useDimension;
