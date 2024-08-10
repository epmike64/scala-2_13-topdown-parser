/*
 * Copyright (c) 1999, 2010, Oracle and/or its affiliates. All rights reserved.
 * ORACLE PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

package com.fdk.compiler.util;


object LayoutCharacters {

    /** Tabulator column increment.
     */
    val TabInc = 8

    /** Standard indentation for subdiagnostics
     */
    val DiagInc = 4

    /** Standard indentation for additional diagnostic lines
     */
    val DetailsInc = 2

    /** Tabulator character.
     */
    val TAB  = 0x9

    /** Line feed character.
     */
    val LF:Byte = 0xA

    /** Form feed character.
     */
    val FF:Byte = 0xC

    /** Carriage return character.
     */
    val CR:Byte    = 0xD

    /** End of input character.  Used as a sentinel to denote the
     *  character one beyond the last defined character in a
     *  source file.
     */
    val EOI:Char   = 0x1A
}
