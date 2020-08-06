import { useState } from 'react';

type ChangeSelected = (code: string)=>void

export default function useSelectedType(initCode: string = ''):[string, ChangeSelected] {
  const [selectedType, setSelectedType] = useState<string>(initCode);
  const handleChangeSelectedType: ChangeSelected = (code) => {
    setSelectedType(code);
  };
  return [selectedType, handleChangeSelectedType];
}
